package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.mobcatcher;

import net.minecraft.ChatFormatting;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.screens.inventory.InventoryScreen;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.Entity;
import net.minecraft.world.entity.LivingEntity;
import net.minecraft.world.inventory.Slot;
import net.neoforged.neoforge.network.PacketDistributor;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.backpack.wrapper.IBackpackWrapper;
import net.p3pp3rf1y.sophisticatedbackpacks.common.gui.BackpackContainer;
import net.p3pp3rf1y.sophisticatedbackpacks.network.MobCatcherReleasePayload;
import net.p3pp3rf1y.sophisticatedcore.client.gui.StorageScreenBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.UpgradeInventoryControlBase;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Dimension;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.TextureBlitData;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.UV;
import org.joml.Quaternionf;
import org.joml.Vector3f;

import java.util.*;

public class MobCatcherInventoryControl extends UpgradeInventoryControlBase {
	private static final int CAPTURED_MOB_BACKGROUND_U = 29;
	private static final int CAPTURED_MOB_BACKGROUND_V = 30;
	private static final int CAPTURED_MOB_BACKGROUND_WIDTH = 18;
	private static final int CAPTURED_MOB_BACKGROUND_HEIGHT = 54;
	private static final int CAPTURED_MOB_BACKGROUND_COLOR = 0xFF_2B2B2B;
	private static final TextureBlitData RELEASE_BUTTON_FOREGROUND = new TextureBlitData(GuiHelper.ICONS, Dimension.SQUARE_256, new UV(12, 156), Dimension.SQUARE_12);
	private static final float BODY_YAW_RANGE = 50F;
	private static final float HEAD_STATIC_YAW_RANGE = 24F;
	private static final float HEAD_IDLE_YAW_AMPLITUDE = 33F;
	private static final float HEAD_STATIC_PITCH_RANGE = 6F;
	private static final float HEAD_IDLE_PITCH_AMPLITUDE = 10F;
	private static final float HEAD_IDLE_MIN_CYCLE_TICKS = 340F;
	private static final float HEAD_IDLE_CYCLE_VARIATION_TICKS = 180F;
	private static final float HEAD_IDLE_MOVE_TICKS = 30F;
	private static final float HEAD_IDLE_HOLD_TICKS = 15F;
	private static final Vector3f ENTITY_RENDER_TRANSLATION = new Vector3f(0.0F, 0.0F, 0.0F);
	private static final Quaternionf ENTITY_RENDER_ANGLE = new Quaternionf().rotateZ((float) Math.PI);
	private static final Quaternionf ENTITY_CAMERA_ANGLE = new Quaternionf().rotateY((float) Math.PI);

	private final StorageScreenBase<?> screen;
	private final BackpackContainer menu;
	private final Map<UUID, LivingEntity> capturedMobRenderEntities = new HashMap<>();
	private final Set<UUID> capturedMobRenderFailures = new HashSet<>();

	public static Optional<MobCatcherInventoryControl> create(StorageScreenBase<?> screen) {
		return screen.getMenu() instanceof BackpackContainer backpackContainer ? Optional.of(new MobCatcherInventoryControl(screen, backpackContainer)) : Optional.empty();
	}

	private MobCatcherInventoryControl(StorageScreenBase<?> screen, BackpackContainer menu) {
		this.screen = screen;
		this.menu = menu;
	}

	@Override
	public void render(GuiGraphics guiGraphics, int mouseX, int mouseY) {
		Optional<CapturedMob> hoveredMob = getHoveredCapturedMob(mouseX, mouseY);
		for (CapturedMob capturedMob : MobCatcherStorage.getCapturedMobs(getBackpackWrapper())) {
			Optional<CapturedMobRenderBounds> renderBounds = getCapturedMobRenderBounds(capturedMob);
			if (renderBounds.isEmpty()) {
				continue;
			}
			CapturedMobRenderBounds bounds = renderBounds.get();
			int x = bounds.x();
			int y = bounds.y();
			int width = bounds.width();
			int height = bounds.height();
			renderCapturedMobArea(guiGraphics, x, y, capturedMob.width(), capturedMob.height());
			getRenderEntity(capturedMob).ifPresent(entity -> {
				int scale = getRenderScale(entity, width, height);
				prepareEntityForRender(entity, capturedMob);
				InventoryScreen.renderEntityInInventory(guiGraphics, x + width / 2F, getRenderBottomY(entity, y, height, scale), scale,
						ENTITY_RENDER_TRANSLATION, ENTITY_RENDER_ANGLE, ENTITY_CAMERA_ANGLE, entity);
			});
			if (hoveredMob.map(mob -> mob.id().equals(capturedMob.id())).orElse(false)) {
				renderReleaseHint(guiGraphics, x, y, width, height);
			}
		}
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int button) {
		if (button != 0) {
			return false;
		}
		Optional<CapturedMob> clickedMob = getHoveredCapturedMob(mouseX, mouseY);
		if (clickedMob.isEmpty()) {
			return false;
		}
		PacketDistributor.sendToServer(new MobCatcherReleasePayload(clickedMob.get().id()));
		return true;
	}

	@Override
	public boolean replacesSlotRender(int slot) {
		IBackpackWrapper backpackWrapper = getBackpackWrapper();
		return MobCatcherStorage.getCapturedMobs(backpackWrapper).stream().anyMatch(capturedMob -> capturedMob.occupiesSlot(slot, MobCatcherStorage.getColumns(backpackWrapper)));
	}

	@Override
	public void renderErrorOverlay(GuiGraphics guiGraphics, Set<Integer> errorInventorySlots) {
		IBackpackWrapper backpackWrapper = getBackpackWrapper();
		int columns = MobCatcherStorage.getColumns(backpackWrapper);
		Optional<CapturedMobRenderBounds> visibleStorageBounds = getVisibleStorageBounds();
		if (visibleStorageBounds.isEmpty()) {
			return;
		}
		for (CapturedMob capturedMob : MobCatcherStorage.getCapturedMobs(backpackWrapper)) {
			Optional<CapturedMobRenderBounds> renderBounds = getCapturedMobRenderBounds(capturedMob);
			if (renderBounds.isEmpty() || errorInventorySlots.stream().noneMatch(slot -> capturedMob.occupiesSlot(slot, columns))) {
				continue;
			}
			getInteriorVisibleBounds(renderBounds.get(), visibleStorageBounds.get()).ifPresent(bounds ->
					screen.renderOverlay(guiGraphics, StorageScreenBase.ERROR_SLOT_COLOR, bounds.x(), bounds.y(), bounds.width(), bounds.height()));
		}
	}

	@Override
	public void renderTooltip(StorageScreenBase<?> screen, GuiGraphics guiGraphics, int mouseX, int mouseY) {
		Optional<CapturedMob> hoveredMob = getHoveredCapturedMob(mouseX, mouseY);
		if (hoveredMob.isEmpty()) {
			return;
		}

		CapturedMob capturedMob = hoveredMob.get();
		Optional<LivingEntity> entity = getRenderEntity(capturedMob);
		List<Component> tooltipLines = new ArrayList<>();
		tooltipLines.add(Component.literal(getTooltipDisplayName(capturedMob, entity)));
		if (capturedMobRenderFailures.contains(capturedMob.id())) {
			tooltipLines.add(Component.translatable("gui.sophisticatedbackpacks.mob_catcher.entity_preview_failed").withStyle(ChatFormatting.RED));
		}
		tooltipLines.add(Component.translatable("gui.sophisticatedbackpacks.mob_catcher.click_to_release").withStyle(ChatFormatting.GRAY, ChatFormatting.ITALIC));
		guiGraphics.renderTooltip(this.screen.getMinecraft().font, tooltipLines, Optional.of(new MobCatcherHealthTooltip(capturedMob.currentHealth(), capturedMob.maxHealth())), mouseX, mouseY);
	}

	private String getTooltipDisplayName(CapturedMob capturedMob, Optional<LivingEntity> entity) {
		return entity.map(livingEntity -> livingEntity.hasCustomName() ? livingEntity.getCustomName().getString() : livingEntity.getType().getDescription().getString()).orElse(capturedMob.displayName());
	}

	private void renderCapturedMobArea(GuiGraphics guiGraphics, int x, int y, int widthSlots, int heightSlots) {
		int backgroundX = x - 1;
		int backgroundY = y - 1;
		int width = widthSlots * 18;
		int height = heightSlots * 18;
		GuiHelper.renderTiledControlBackground(guiGraphics, backgroundX, backgroundY, width, height, CAPTURED_MOB_BACKGROUND_U, CAPTURED_MOB_BACKGROUND_V, CAPTURED_MOB_BACKGROUND_WIDTH, CAPTURED_MOB_BACKGROUND_HEIGHT);
		renderCapturedMobBackgroundGradient(guiGraphics, backgroundX + 1, backgroundY + 1, width - 2, height - 2);
	}

	private void renderCapturedMobBackgroundGradient(GuiGraphics guiGraphics, int x, int y, int width, int height) {
		guiGraphics.fill(x, y, x + width, y + height, CAPTURED_MOB_BACKGROUND_COLOR);
		int layers = Math.max(1, Math.min(5, Math.min(width, height) / 5));
		for (int layer = 0; layer < layers; layer++) {
			int insetX = 1 + layer * width / (layers * 3);
			int insetY = 1 + layer * height / (layers * 3);
			int alpha = 24 + layer * 12;
			guiGraphics.fill(x + insetX, y + insetY, x + width - insetX, y + height - insetY, alpha << 24 | 0x4A4A4A);
		}
	}

	private void renderReleaseHint(GuiGraphics guiGraphics, int x, int y, int width, int height) {
		int releaseX = x + width - 14;
		int releaseY = y + height - 14;
		GuiHelper.blit(guiGraphics, releaseX, releaseY, RELEASE_BUTTON_FOREGROUND);
	}

	private void prepareEntityForRender(LivingEntity entity, CapturedMob capturedMob) {
		float renderTime = getRenderTime(capturedMob);
		float bodyRot = 180F + (getUuidFloat(capturedMob.id(), 0) - 0.5F) * BODY_YAW_RANGE;
		float headOffset = -HEAD_STATIC_YAW_RANGE / 2F + getUuidFloat(capturedMob.id(), 1) * HEAD_STATIC_YAW_RANGE + getIdlePoseOffset(capturedMob.id(), renderTime, 0, HEAD_IDLE_YAW_AMPLITUDE);
		float pitch = -HEAD_STATIC_PITCH_RANGE / 2F + getUuidFloat(capturedMob.id(), 2) * HEAD_STATIC_PITCH_RANGE + getIdlePoseOffset(capturedMob.id(), renderTime, 1, HEAD_IDLE_PITCH_AMPLITUDE);
		entity.tickCount = (int) renderTime;
		entity.setYRot(bodyRot);
		entity.yRotO = bodyRot;
		entity.yBodyRot = bodyRot;
		entity.yBodyRotO = bodyRot;
		entity.yHeadRot = bodyRot + headOffset;
		entity.yHeadRotO = entity.yHeadRot;
		entity.setXRot(pitch);
		entity.xRotO = pitch;
	}

	private float getRenderTime(CapturedMob capturedMob) {
		return (screen.getMinecraft().player == null ? 0 : screen.getMinecraft().player.tickCount + screen.getMinecraft().getTimer().getGameTimeDeltaPartialTick(false)) + getUuidFloat(capturedMob.id(), 3) * 200F;
	}

	private float getIdlePoseOffset(UUID uuid, float renderTime, int salt, float amplitude) {
		float cycleLength = HEAD_IDLE_MIN_CYCLE_TICKS + getUuidFloat(uuid, salt + 4) * HEAD_IDLE_CYCLE_VARIATION_TICKS;
		float moveTicks = HEAD_IDLE_MOVE_TICKS;
		float holdTicks = HEAD_IDLE_HOLD_TICKS;
		int cycle = (int) Math.floor(renderTime / cycleLength);
		float phase = renderTime - cycle * cycleLength;
		float target = (getCycleFloat(uuid, cycle, salt) - 0.5F) * 2F * amplitude;
		if (phase < moveTicks) {
			return smoothStep(phase / moveTicks) * target;
		}
		if (phase < moveTicks + holdTicks) {
			return target;
		}
		if (phase < moveTicks * 2F + holdTicks) {
			return (1F - smoothStep((phase - moveTicks - holdTicks) / moveTicks)) * target;
		}
		return 0;
	}

	private float smoothStep(float value) {
		return value * value * (3F - 2F * value);
	}

	private float getCycleFloat(UUID uuid, int cycle, int salt) {
		return Math.floorMod(uuid.hashCode() * 31 + cycle * 131 + salt * 17, 1000) / 999F;
	}

	private float getUuidFloat(UUID uuid, int salt) {
		return Math.floorMod(uuid.hashCode() + salt * 31, 1000) / 999F;
	}

	private int getRenderScale(LivingEntity entity, int width, int height) {
		float entityWidth = Math.max(entity.getBbWidth(), 0.25F);
		float entityHeight = Math.max(entity.getBbHeight(), 0.25F);
		float scale = Math.min((width - 6) / entityWidth, (height - 6) / entityHeight) * 0.5625F;
		return Math.max(8, (int) scale);
	}

	private float getRenderBottomY(LivingEntity entity, int y, int height, int scale) {
		return y + height / 2F + entity.getBbHeight() * scale / 2F;
	}

	private Optional<CapturedMob> getHoveredCapturedMob(double mouseX, double mouseY) {
		return MobCatcherStorage.getCapturedMobs(getBackpackWrapper()).stream()
				.filter(capturedMob -> isMouseOverVisibleCapturedMob(capturedMob, mouseX, mouseY))
				.findFirst();
	}

	private boolean isMouseOverVisibleCapturedMob(CapturedMob capturedMob, double mouseX, double mouseY) {
		int columns = MobCatcherStorage.getColumns(getBackpackWrapper());
		for (int yOffset = 0; yOffset < capturedMob.height(); yOffset++) {
			for (int xOffset = 0; xOffset < capturedMob.width(); xOffset++) {
				int slotIndex = capturedMob.slot() + yOffset * columns + xOffset;
				if (slotIndex >= menu.getNumberOfStorageInventorySlots()) {
					continue;
				}

				Slot slot = menu.getSlot(slotIndex);
				int x = slot.x + screen.getGuiLeft() - 1;
				int y = slot.y + screen.getGuiTop() - 1;
				if (isSlotVisible(slot) && mouseX >= x && mouseX < x + 18 && mouseY >= y && mouseY < y + 18) {
					return true;
				}
			}
		}
		return false;
	}

	private Optional<CapturedMobRenderBounds> getCapturedMobRenderBounds(CapturedMob capturedMob) {
		int columns = MobCatcherStorage.getColumns(getBackpackWrapper());
		for (int yOffset = 0; yOffset < capturedMob.height(); yOffset++) {
			for (int xOffset = 0; xOffset < capturedMob.width(); xOffset++) {
				int slotIndex = capturedMob.slot() + yOffset * columns + xOffset;
				if (slotIndex >= menu.getNumberOfStorageInventorySlots()) {
					continue;
				}

				Slot slot = menu.getSlot(slotIndex);
				if (isSlotVisible(slot)) {
					return Optional.of(new CapturedMobRenderBounds(slot.x - xOffset * 18, slot.y - yOffset * 18, capturedMob.width() * 18, capturedMob.height() * 18));
				}
			}
		}
		return Optional.empty();
	}

	private Optional<CapturedMobRenderBounds> getVisibleStorageBounds() {
		int left = Integer.MAX_VALUE;
		int top = Integer.MAX_VALUE;
		int right = Integer.MIN_VALUE;
		int bottom = Integer.MIN_VALUE;
		for (int slotIndex = 0; slotIndex < menu.getNumberOfStorageInventorySlots(); slotIndex++) {
			Slot slot = menu.getSlot(slotIndex);
			if (!isSlotVisible(slot)) {
				continue;
			}

			left = Math.min(left, slot.x);
			top = Math.min(top, slot.y);
			right = Math.max(right, slot.x + 16);
			bottom = Math.max(bottom, slot.y + 16);
		}
		return right <= left || bottom <= top ? Optional.empty() : Optional.of(new CapturedMobRenderBounds(left, top, right - left, bottom - top));
	}

	private Optional<CapturedMobRenderBounds> getInteriorVisibleBounds(CapturedMobRenderBounds capturedMobBounds, CapturedMobRenderBounds visibleStorageBounds) {
		int visibleBottom = visibleStorageBounds.y() + visibleStorageBounds.height();
		int capturedBottom = capturedMobBounds.y() + capturedMobBounds.height();
		int left = Math.max(capturedMobBounds.x(), visibleStorageBounds.x());
		int top = capturedMobBounds.y() < visibleStorageBounds.y() ? visibleStorageBounds.y() - 1 : capturedMobBounds.y();
		int right = Math.min(capturedMobBounds.x() + capturedMobBounds.width() - 2, visibleStorageBounds.x() + visibleStorageBounds.width());
		int bottom = capturedBottom > visibleBottom ? visibleBottom + 1 : capturedBottom - 2;
		return right <= left || bottom <= top ? Optional.empty() : Optional.of(new CapturedMobRenderBounds(left, top, right - left, bottom - top));
	}

	private boolean isSlotVisible(Slot slot) {
		return slot.x != StorageScreenBase.DISABLED_SLOT_X_POS && slot.y >= 0;
	}

	private IBackpackWrapper getBackpackWrapper() {
		return menu.getStorageWrapper();
	}

	private record CapturedMobRenderBounds(int x, int y, int width, int height) {}

	private Optional<LivingEntity> getRenderEntity(CapturedMob capturedMob) {
		if (capturedMobRenderFailures.contains(capturedMob.id())) {
			return Optional.empty();
		}

		LivingEntity cachedEntity = capturedMobRenderEntities.get(capturedMob.id());
		if (cachedEntity != null) {
			return Optional.of(cachedEntity);
		}

		try {
			Optional<Entity> entity = MobCatcherStorage.getEntityType(capturedMob).map(entityType -> entityType.create(screen.getMinecraft().level));
			if (entity.isEmpty()) {
				return Optional.empty();
			}
			if (!(entity.get() instanceof LivingEntity livingEntity)) {
				return Optional.empty();
			}
			livingEntity.load(capturedMob.entityNbt());
			capturedMobRenderEntities.put(capturedMob.id(), livingEntity);
			return Optional.of(livingEntity);
		} catch (RuntimeException e) {
			capturedMobRenderFailures.add(capturedMob.id());
			SophisticatedBackpacks.LOGGER.warn("Unable to create render entity for captured mob {} ({})", capturedMob.displayName(), capturedMob.entityType(), e);
			return Optional.empty();
		}
	}
}

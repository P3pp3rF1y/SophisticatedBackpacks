package net.p3pp3rf1y.sophisticatedcore.client.gui;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import com.mojang.blaze3d.vertex.Tesselator;
import com.mojang.datafixers.util.Pair;
import com.mojang.math.Matrix4f;
import it.unimi.dsi.fastutil.ints.Int2ObjectMap;
import it.unimi.dsi.fastutil.ints.Int2ObjectOpenHashMap;
import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.Font;
import net.minecraft.client.gui.components.events.GuiEventListener;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.client.renderer.MultiBufferSource;
import net.minecraft.client.renderer.Rect2i;
import net.minecraft.client.renderer.texture.TextureAtlasSprite;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.FormattedText;
import net.minecraft.network.chat.Style;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.network.protocol.game.ServerboundContainerClickPacket;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.inventory.AbstractContainerMenu;
import net.minecraft.world.inventory.ClickType;
import net.minecraft.world.inventory.Slot;
import net.minecraft.world.item.DyeColor;
import net.minecraft.world.item.ItemStack;
import net.p3pp3rf1y.sophisticatedcore.Config;
import net.p3pp3rf1y.sophisticatedcore.SophisticatedCore;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.Button;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ButtonDefinitions;
import net.p3pp3rf1y.sophisticatedcore.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper;
import net.p3pp3rf1y.sophisticatedcore.client.gui.utils.Position;
import net.p3pp3rf1y.sophisticatedcore.common.gui.SortBy;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageBackgroundProperties;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageContainerMenuBase;
import net.p3pp3rf1y.sophisticatedcore.common.gui.StorageInventorySlot;
import net.p3pp3rf1y.sophisticatedcore.common.gui.UpgradeContainerBase;
import net.p3pp3rf1y.sophisticatedcore.network.TransferFullSlotMessage;
import net.p3pp3rf1y.sophisticatedcore.upgrades.crafting.ICraftingUIPart;
import net.p3pp3rf1y.sophisticatedcore.util.ColorHelper;
import net.p3pp3rf1y.sophisticatedcore.util.CountAbbreviator;

import javax.annotation.Nullable;
import java.text.NumberFormat;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.concurrent.atomic.AtomicReference;

import static net.p3pp3rf1y.sophisticatedcore.client.gui.utils.GuiHelper.GUI_CONTROLS;

public abstract class StorageScreenBase<S extends StorageContainerMenuBase<?>> extends AbstractContainerScreen<S> {
	public static final int ERROR_BACKGROUND_COLOR = 0xF0100010;
	public static final int ERROR_BORDER_COLOR = ColorHelper.getColor(DyeColor.RED.getTextureDiffuseColors()) | 0xFF000000;
	private static final int DISABLED_SLOT_COLOR = -1072689136;
	private static final int UPGRADE_TOP_HEIGHT = 7;
	private static final int UPGRADE_SLOT_HEIGHT = 18;
	private static final int UPGRADE_SPACE_BETWEEN_SLOTS = 4;
	private static final int UPGRADE_BOTTOM_HEIGHT = 7;
	private static final int TOTAL_UPGRADE_GUI_HEIGHT = 252;
	public static final int UPGRADE_INVENTORY_OFFSET = 26;
	public static final int DISABLED_SLOT_X_POS = -1000;
	static final int SLOTS_Y_OFFSET = 17;
	static final int SLOTS_X_OFFSET = 7;
	public static final int ERROR_SLOT_COLOR = ColorHelper.getColor(DyeColor.RED.getTextureDiffuseColors()) | 0xAA000000;
	private static final int ERROR_TEXT_COLOR = ColorHelper.getColor(DyeColor.RED.getTextureDiffuseColors());

	private UpgradeSettingsTabControl settingsTabControl;
	private final int numberOfUpgradeSlots;
	@Nullable
	private Button sortButton = null;
	@Nullable
	private ToggleButton<SortBy> sortByButton = null;
	private final Set<ToggleButton<Boolean>> upgradeSwitches = new HashSet<>();

	private final Map<Integer, UpgradeInventoryPartBase<?>> inventoryParts = new LinkedHashMap<>();

	private static ICraftingUIPart craftingUIPart = ICraftingUIPart.NOOP;

	public static void setCraftingUIPart(ICraftingUIPart part) {
		craftingUIPart = part;
	}

	protected StorageScreenBase(S pMenu, Inventory pPlayerInventory, Component pTitle) {
		super(pMenu, pPlayerInventory, pTitle);
		imageHeight = 114 + getMenu().getNumberOfRows() * 18;
		imageWidth = getMenu().getStorageBackgroundProperties().getSlotsOnLine() * 18 + 14;
		inventoryLabelY = imageHeight - 94;
		inventoryLabelX = 8 + getMenu().getStorageBackgroundProperties().getPlayerInventoryXOffset();
		numberOfUpgradeSlots = getMenu().getNumberOfUpgradeSlots();
		passEvents = true;
	}

	public ICraftingUIPart getCraftingUIAddition() {
		return craftingUIPart;
	}

	@Override
	protected void init() {
		super.init();
		craftingUIPart.setStorageScreen(this);
		initUpgradeSettingsControl();
		initUpgradeInventoryParts();
		addUpgradeSwitches();
		getMenu().setUpgradeChangeListener(c -> {
			children().remove(settingsTabControl);
			craftingUIPart.onCraftingSlotsHidden();
			initUpgradeSettingsControl();
			initUpgradeInventoryParts();
			addUpgradeSwitches();
		});
		addSortButtons();
	}

	private void initUpgradeInventoryParts() {
		inventoryParts.clear();
		if (getMenu().getColumnsTaken() == 0) {
			return;
		}

		AtomicReference<Position> pos = new AtomicReference<>(new Position(SLOTS_X_OFFSET + menu.getSlotsOnLine() * 18, SLOTS_Y_OFFSET));
		int height = menu.getNumberOfRows() * 18;
		for (Map.Entry<Integer, UpgradeContainerBase<?, ?>> entry : getMenu().getUpgradeContainers().entrySet()) {
			UpgradeContainerBase<?, ?> container = entry.getValue();
			UpgradeGuiManager.getInventoryPart(entry.getKey(), container, pos.get(), height, this).ifPresent(part -> {
				inventoryParts.put(entry.getKey(), part);
				pos.set(new Position(pos.get().x() + 36, pos.get().y()));
			});
		}
	}

	private void addUpgradeSwitches() {
		upgradeSwitches.clear();
		int switchTop = topPos + getUpgradeTop() + 10;
		for (int slot = 0; slot < numberOfUpgradeSlots; slot++) {
			if (menu.canDisableUpgrade(slot)) {
				int finalSlot = slot;
				ToggleButton<Boolean> upgradeSwitch = new ToggleButton<>(new Position(leftPos - 22, switchTop), ButtonDefinitions.UPGRADE_SWITCH,
						button -> getMenu().setUpgradeEnabled(finalSlot, !getMenu().getUpgradeEnabled(finalSlot)), () -> getMenu().getUpgradeEnabled(finalSlot));
				addWidget(upgradeSwitch);
				upgradeSwitches.add(upgradeSwitch);
			}
			switchTop += 22;
		}
	}

	private void addSortButtons() {
		SortButtonsPosition sortButtonsPosition = Config.CLIENT.sortButtonsPosition.get();
		if (sortButtonsPosition == SortButtonsPosition.HIDDEN) {
			return;
		}

		Position pos = getSortButtonsPosition(sortButtonsPosition);

		sortButton = new Button(new Position(pos.x(), pos.y()), ButtonDefinitions.SORT, button -> {
			if (button == 0) {
				getMenu().sort();
				Minecraft.getInstance().player.displayClientMessage(new TextComponent("Sorted"), true);
			}
		});
		addWidget(sortButton);
		sortByButton = new ToggleButton<>(new Position(pos.x() + 14, pos.y()), ButtonDefinitions.SORT_BY, button -> {
			if (button == 0) {
				getMenu().setSortBy(getMenu().getSortBy().next());
			}
		}, () -> getMenu().getSortBy());
		addWidget(sortByButton);

	}

	private Position getSortButtonsPosition(SortButtonsPosition sortButtonsPosition) {
		return switch (sortButtonsPosition) {
			case ABOVE_UPGRADES -> new Position(leftPos - UPGRADE_INVENTORY_OFFSET - 2, topPos + getUpgradeTop() - 14);
			case BELOW_UPGRADES -> new Position(leftPos - UPGRADE_INVENTORY_OFFSET - 2, topPos + getUpgradeTop() + getUpgradeHeightWithoutBottom() + UPGRADE_BOTTOM_HEIGHT + 2);
			case BELOW_UPGRADE_TABS -> settingsTabControl == null ? new Position(0, 0) : new Position(settingsTabControl.getX() + 2, settingsTabControl.getY() + Math.max(0, settingsTabControl.getHeight() + 2));
			case TITLE_LINE_RIGHT -> new Position(leftPos + imageWidth - 34, topPos + 4);
			default -> new Position(leftPos + imageWidth - 34, topPos + 4);
		};
	}

	private void initUpgradeSettingsControl() {
		settingsTabControl = new UpgradeSettingsTabControl(new Position(leftPos + imageWidth, topPos + 4), this, getStorageSettingsTabTooltip());
		addWidget(settingsTabControl);
	}

	protected abstract String getStorageSettingsTabTooltip();

	public int getUpgradeTop() {
		return imageHeight - 94 - getUpgradeHeight();
	}

	public int getUpgradeHeight() {
		return getUpgradeHeightWithoutBottom() + UPGRADE_TOP_HEIGHT;
	}

	protected int getUpgradeHeightWithoutBottom() {
		return UPGRADE_BOTTOM_HEIGHT + numberOfUpgradeSlots * UPGRADE_SLOT_HEIGHT + (numberOfUpgradeSlots - 1) * UPGRADE_SPACE_BETWEEN_SLOTS;
	}

	public Optional<Rect2i> getSortButtonsRectangle() {
		return sortButton == null || sortByButton == null ? Optional.empty() : Optional.of(new Rect2i(sortButton.getX(), sortButton.getY(),
				sortByButton.getX() + sortByButton.getWidth() - sortButton.getX(), sortByButton.getY() + sortByButton.getHeight() - sortButton.getY()));
	}

	@Override
	public void render(PoseStack matrixStack, int mouseX, int mouseY, float partialTicks) {
		menu.detectSettingsChangeAndReload();
		renderBackground(matrixStack);
		settingsTabControl.render(matrixStack, mouseX, mouseY, partialTicks);
		matrixStack.translate(0, 0, 200);
		super.render(matrixStack, mouseX, mouseY, partialTicks);
		settingsTabControl.renderTooltip(this, matrixStack, mouseX, mouseY);
		if (sortButton != null && sortByButton != null) {
			sortButton.render(matrixStack, mouseX, mouseY, partialTicks);
			sortByButton.render(matrixStack, mouseX, mouseY, partialTicks);
		}
		upgradeSwitches.forEach(us -> us.render(matrixStack, mouseX, mouseY, partialTicks));
		renderErrorOverlay(matrixStack);
		renderTooltip(matrixStack, mouseX, mouseY);
	}

	@Override
	protected void renderLabels(PoseStack matrixStack, int mouseX, int mouseY) {
		super.renderLabels(matrixStack, mouseX, mouseY);
		renderUpgradeInventoryParts(matrixStack, mouseX, mouseY);
		renderUpgradeSlots(matrixStack, mouseX, mouseY);
		renderRealInventorySlots(matrixStack, mouseX, mouseY);
	}

	private void renderUpgradeInventoryParts(PoseStack matrixStack, int mouseX, int mouseY) {
		inventoryParts.values().forEach(ip -> ip.render(matrixStack, mouseX, mouseY));
	}

	private void renderRealInventorySlots(PoseStack matrixStack, int mouseX, int mouseY) {
		for (int slotId = 0; slotId < menu.realInventorySlots.size(); ++slotId) {
			Slot slot = menu.realInventorySlots.get(slotId);
			renderSlot(matrixStack, slot);

			if (isHovering(slot, mouseX, mouseY) && slot.isActive()) {
				hoveredSlot = slot;
				renderSlotOverlay(matrixStack, slot, getSlotColor(slotId));
			}
		}
	}

	private void renderUpgradeSlots(PoseStack matrixStack, int mouseX, int mouseY) {
		for (int slotId = 0; slotId < menu.upgradeSlots.size(); ++slotId) {
			Slot slot = menu.upgradeSlots.get(slotId);
			if (slot.x != DISABLED_SLOT_X_POS) {
				renderSlot(matrixStack, slot);
				if (!slot.isActive()) {
					renderSlotOverlay(matrixStack, slot, DISABLED_SLOT_COLOR);
				}
			}

			if (isHovering(slot, mouseX, mouseY) && slot.isActive()) {
				hoveredSlot = slot;
				renderSlotOverlay(matrixStack, slot, getSlotColor(slotId));
			}
		}
	}

	@Override
	protected void renderSlot(PoseStack matrixStack, Slot slot) {
		int i = slot.x;
		int j = slot.y;
		ItemStack itemstack = slot.getItem();
		boolean flag = false;
		boolean rightClickDragging = slot == clickedSlot && !draggingItem.isEmpty() && !isSplittingStack;
		ItemStack itemstack1 = getMenu().getCarried();
		String stackCountText = null;
		if (slot == clickedSlot && !draggingItem.isEmpty() && isSplittingStack && !itemstack.isEmpty()) {
			itemstack = itemstack.copy();
			itemstack.setCount(itemstack.getCount() / 2);
		} else if (isQuickCrafting && quickCraftSlots.contains(slot) && !itemstack1.isEmpty()) {
			if (quickCraftSlots.size() == 1) {
				return;
			}

			if (StorageContainerMenuBase.canItemQuickReplace(slot, itemstack1) && menu.canDragTo(slot)) {
				itemstack = itemstack1.copy();
				flag = true;
				AbstractContainerMenu.getQuickCraftSlotCount(quickCraftSlots, quickCraftingType, itemstack, slot.getItem().isEmpty() ? 0 : slot.getItem().getCount());
				int slotLimit = slot.getMaxStackSize(itemstack);
				if (itemstack.getCount() > slotLimit) {
					stackCountText = ChatFormatting.YELLOW + CountAbbreviator.abbreviate(slotLimit);
					itemstack.setCount(slotLimit);
				}
			} else {
				quickCraftSlots.remove(slot);
				recalculateQuickCraftRemaining();
			}
		}

		setBlitOffset(100);
		itemRenderer.blitOffset = 100.0F;
		if (itemstack.isEmpty() && slot.isActive()) {
			renderSlotBackground(matrixStack, slot, i, j);
		} else if (!rightClickDragging) {
			renderStack(matrixStack, i, j, itemstack, flag, stackCountText);
		}

		itemRenderer.blitOffset = 0.0F;
		setBlitOffset(0);
	}

	private void renderStack(PoseStack poseStack, int i, int j, ItemStack itemstack, boolean flag, @Nullable String stackCountText) {
		if (flag) {
			fill(poseStack, i, j, i + 16, j + 16, -2130706433);
		}

		RenderSystem.enableDepthTest();
		itemRenderer.renderAndDecorateItem(itemstack, i, j);
		if (shouldUseSpecialCountRender(itemstack)) {
			itemRenderer.renderGuiItemDecorations(font, itemstack, i, j, "");
			if (stackCountText == null) {
				stackCountText = CountAbbreviator.abbreviate(itemstack.getCount());
			}
			renderStackCount(stackCountText, i, j);
		} else {
			itemRenderer.renderGuiItemDecorations(font, itemstack, i, j, stackCountText);
		}
	}

	private void renderSlotBackground(PoseStack poseStack, Slot slot, int i, int j) {
		Optional<ItemStack> memorizedStack = getMenu().getMemorizedStackInSlot(slot.index);
		if (memorizedStack.isPresent()) {
			itemRenderer.renderAndDecorateItem(memorizedStack.get(), i, j);
			drawMemorizedStackOverlay(poseStack, i, j);
		} else {
			Pair<ResourceLocation, ResourceLocation> pair = slot.getNoItemIcon();
			if (pair != null) {
				TextureAtlasSprite textureatlassprite = minecraft.getTextureAtlas(pair.getFirst()).apply(pair.getSecond());
				RenderSystem.setShader(GameRenderer::getPositionTexShader);
				RenderSystem.setShaderTexture(0, textureatlassprite.atlas().location());
				blit(poseStack, i, j, getBlitOffset(), 16, 16, textureatlassprite);
			}
		}
	}

	private void drawMemorizedStackOverlay(PoseStack poseStack, int x, int y) {
		poseStack.pushPose();
		RenderSystem.enableBlend();
		RenderSystem.disableDepthTest();
		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderTexture(0, GuiHelper.GUI_CONTROLS);
		blit(poseStack, x, y, 77, 0, 16, 16);
		RenderSystem.enableDepthTest();
		RenderSystem.disableBlend();
		poseStack.popPose();
	}

	private boolean shouldUseSpecialCountRender(ItemStack itemstack) {
		return itemstack.getCount() > 99;
	}

	private void renderSlotOverlay(PoseStack matrixStack, Slot slot, int slotColor) {
		renderSlotOverlay(matrixStack, slot, slotColor, 0, 16);
	}

	private void renderSlotOverlay(PoseStack matrixStack, Slot slot, int slotColor, int yOffset, int height) {
		renderOverlay(matrixStack, slotColor, slot.x, slot.y + yOffset, 16, height);
	}

	public void renderOverlay(PoseStack matrixStack, int slotColor, int xPos, int yPos, int width, int height) {
		RenderSystem.disableDepthTest();
		RenderSystem.colorMask(true, true, true, false);
		fillGradient(matrixStack, xPos, yPos, xPos + width, yPos + height, slotColor, slotColor);
		RenderSystem.colorMask(true, true, true, true);
		RenderSystem.enableDepthTest();
	}

	protected void renderBg(PoseStack matrixStack, float partialTicks, int x, int y) {
		drawInventoryBackground(matrixStack);
		drawUpgradeBackground(matrixStack);
	}

	@Override
	protected void renderTooltip(PoseStack poseStack, int x, int y) {
		poseStack.pushPose();
		poseStack.translate(0, 0, -100);
		inventoryParts.values().forEach(part -> part.renderTooltip(this, poseStack, x, y));
		if (getMenu().getCarried().isEmpty() && hoveredSlot != null) {
			if (hoveredSlot.hasItem()) {
				renderTooltip(poseStack, hoveredSlot.getItem(), x, y);
			} else if (hoveredSlot instanceof INameableEmptySlot emptySlot && emptySlot.hasEmptyTooltip()) {
				renderComponentTooltip(poseStack, Collections.singletonList(emptySlot.getEmptyTooltip()), x, y, font);
			}
		}
		if (sortButton != null) {
			sortButton.renderTooltip(this, poseStack, x, y);
		}
		if (sortByButton != null) {
			sortByButton.renderTooltip(this, poseStack, x, y);
		}
		poseStack.popPose();
	}

	@Override
	public List<Component> getTooltipFromItem(ItemStack itemStack) {
		List<Component> ret = super.getTooltipFromItem(itemStack);
		if (itemStack.getCount() > 999) {
			ret.add(new TranslatableComponent("gui.sophisticatedcore.tooltip.stack_count",
					new TextComponent(NumberFormat.getNumberInstance().format(itemStack.getCount())).withStyle(ChatFormatting.DARK_AQUA))
					.withStyle(ChatFormatting.GRAY)
			);
		}
		return ret;
	}

	private void drawInventoryBackground(PoseStack matrixStack) {
		StorageBackgroundProperties storageBackgroundProperties = getMenu().getStorageBackgroundProperties();
		StorageGuiHelper.renderStorageBackground(new Position((width - imageWidth) / 2, (height - imageHeight) / 2), matrixStack, getMenu().getNumberOfStorageInventorySlots(), getMenu().getSlotsOnLine(), storageBackgroundProperties.getTextureName(), imageWidth, menu.getNumberOfRows());

		matrixStack.pushPose();
		matrixStack.translate(getGuiLeft(), getGuiTop(), 0.0F);
		for (int slotNumber = 0; slotNumber < menu.getNumberOfStorageInventorySlots(); slotNumber++) {
			List<Integer> colors = menu.getSlotOverlayColors(slotNumber);
			if (!colors.isEmpty()) {
				int stripeHeight = 16 / colors.size();
				int i = 0;
				for (int slotColor : colors) {
					int yOffset = i * stripeHeight;
					renderSlotOverlay(matrixStack, menu.getSlot(slotNumber), slotColor | (80 << 24), yOffset, i == colors.size() - 1 ? 16 - yOffset : stripeHeight);
					i++;
				}
			}
		}
		matrixStack.popPose();
	}

	private void drawUpgradeBackground(PoseStack matrixStack) {
		if (numberOfUpgradeSlots == 0) {
			return;
		}

		RenderSystem.setShader(GameRenderer::getPositionTexShader);
		RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
		RenderSystem.setShaderTexture(0, GUI_CONTROLS);

		int firstHalfHeight = getUpgradeHeightWithoutBottom();

		blit(matrixStack, leftPos - UPGRADE_INVENTORY_OFFSET, topPos + getUpgradeTop(), 0, 0, 29, firstHalfHeight, 256, 256);
		blit(matrixStack, leftPos - UPGRADE_INVENTORY_OFFSET, topPos + getUpgradeTop() + firstHalfHeight, 0, (float) TOTAL_UPGRADE_GUI_HEIGHT - UPGRADE_BOTTOM_HEIGHT, 29, UPGRADE_BOTTOM_HEIGHT, 256, 256);
	}

	public UpgradeSettingsTabControl getUpgradeSettingsControl() {
		if (settingsTabControl == null) {
			settingsTabControl = new UpgradeSettingsTabControl(new Position(leftPos + imageWidth, topPos + 4), this, getStorageSettingsTabTooltip());
		}
		return settingsTabControl;
	}

	@Nullable
	@Override
	public Slot findSlot(double mouseX, double mouseY) {
		for (int i = 0; i < menu.upgradeSlots.size(); ++i) {
			Slot slot = menu.upgradeSlots.get(i);
			if (isHovering(slot, mouseX, mouseY) && slot.isActive()) {
				return slot;
			}
		}

		for (int i = 0; i < menu.realInventorySlots.size(); ++i) {
			Slot slot = menu.realInventorySlots.get(i);
			if (isHovering(slot, mouseX, mouseY) && slot.isActive()) {
				return slot;
			}
		}

		return super.findSlot(mouseX, mouseY);
	}

	@Override
	public boolean mouseReleased(double mouseX, double mouseY, int button) {
		for (UpgradeInventoryPartBase<?> inventoryPart : inventoryParts.values()) {
			if (inventoryPart.handleMouseReleased(mouseX, mouseY, button)) {
				return true;
			}
		}

		handleQuickMoveAll(mouseX, mouseY, button);

		return super.mouseReleased(mouseX, mouseY, button);
	}

	private void handleQuickMoveAll(double mouseX, double mouseY, int button) {
		Slot slot = findSlot(mouseX, mouseY);
		if (doubleclick && !getMenu().getCarried().isEmpty() && slot != null && button == 0 && menu.canTakeItemForPickAll(ItemStack.EMPTY, slot) && hasShiftDown() && !lastQuickMoved.isEmpty()) {
			for (Slot slot2 : menu.realInventorySlots) {
				tryQuickMoveSlot(button, slot, slot2);
			}
		}
	}

	private void tryQuickMoveSlot(int button, Slot slot, Slot slot2) {
		if (slot2.mayPickup(minecraft.player) && slot2.hasItem() && slot2.isSameInventory(slot)) {
			ItemStack slotItem = slot2.getItem();
			if (ItemStack.isSameItemSameTags(lastQuickMoved, slotItem)) {
				if (slotItem.getCount() > slotItem.getMaxStackSize()) {
					SophisticatedCore.PACKET_HANDLER.sendToServer(new TransferFullSlotMessage(slot2.index));
				} else {
					slotClicked(slot2, slot2.index, button, ClickType.QUICK_MOVE);
				}
			}
		}
	}

	@Override
	protected void slotClicked(Slot slot, int slotNumber, int mouseButton, ClickType type) {
		if (type == ClickType.PICKUP_ALL && !menu.getSlotUpgradeContainer(slot).map(c -> c.allowsPickupAll(slot)).orElse(true)) {
			type = ClickType.PICKUP;
		}

		handleInventoryMouseClick(slotNumber, mouseButton, type);
	}

	private void handleInventoryMouseClick(int slotNumber, int mouseButton, ClickType type) {
		StorageContainerMenuBase<?> menu = getMenu();
		List<ItemStack> realInventoryItems = new ArrayList<>(menu.realInventorySlots.size());
		menu.realInventorySlots.forEach(slot -> realInventoryItems.add(slot.getItem().copy()));
		List<ItemStack> upgradeItems = new ArrayList<>(menu.upgradeSlots.size());
		menu.upgradeSlots.forEach(slot -> upgradeItems.add(slot.getItem().copy()));

		menu.clicked(slotNumber, mouseButton, type, minecraft.player);
		Int2ObjectMap<ItemStack> changedSlotIndexes = new Int2ObjectOpenHashMap<>();

		int inventorySlotsToCheck = Math.min(realInventoryItems.size() - StorageContainerMenuBase.NUMBER_OF_PLAYER_SLOTS, menu.getInventorySlotsSize() - StorageContainerMenuBase.NUMBER_OF_PLAYER_SLOTS);

		for (int i = 0; i < inventorySlotsToCheck; i++) {
			ItemStack itemstack = realInventoryItems.get(i);
			ItemStack slotStack = menu.getSlot(i).getItem();
			if (!ItemStack.matches(itemstack, slotStack)) {
				changedSlotIndexes.put(i, slotStack.copy());
			}
		}

		for (int i = 0; i < StorageContainerMenuBase.NUMBER_OF_PLAYER_SLOTS; i++) {
			ItemStack itemstack = realInventoryItems.get(realInventoryItems.size() - StorageContainerMenuBase.NUMBER_OF_PLAYER_SLOTS + i);
			int slotIndex = menu.getInventorySlotsSize() - StorageContainerMenuBase.NUMBER_OF_PLAYER_SLOTS + i;
			ItemStack slotStack = menu.getSlot(slotIndex).getItem();
			if (!ItemStack.matches(itemstack, slotStack)) {
				changedSlotIndexes.put(slotIndex, slotStack.copy());
			}
		}

		int lastChecked = 0;
		int upgradeSlotsToCheck = Math.min(menu.getUpgradeSlotsSize(), upgradeItems.size());

		for (; lastChecked < upgradeSlotsToCheck; lastChecked++) {
			ItemStack itemstack = upgradeItems.get(lastChecked);
			ItemStack slotStack = menu.getSlot(menu.getInventorySlotsSize() + lastChecked).getItem();
			if (!ItemStack.matches(itemstack, slotStack)) {
				break;
			}
		}

		for (int i = upgradeSlotsToCheck - 1; i >= lastChecked; i--) {
			ItemStack itemstack = upgradeItems.get(i);
			int slotIndex = menu.getInventorySlotsSize() + i;
			ItemStack slotStack = menu.getSlot(slotIndex).getItem();
			if (!ItemStack.matches(itemstack, slotStack)) {
				changedSlotIndexes.put(slotIndex, slotStack.copy());
			}
		}

		minecraft.player.connection.send(new ServerboundContainerClickPacket(menu.containerId, menu.getStateId(), slotNumber, mouseButton, type, menu.getCarried().copy(), changedSlotIndexes));
	}

	@Override
	public boolean mouseClicked(double mouseX, double mouseY, int button) {
		Slot slot = findSlot(mouseX, mouseY);
		if (hasShiftDown() && hasControlDown() && slot instanceof StorageInventorySlot && button == 0) {
			SophisticatedCore.PACKET_HANDLER.sendToServer(new TransferFullSlotMessage(slot.index));
			return true;
		}

		return super.mouseClicked(mouseX, mouseY, button);
	}

	@Override
	public boolean mouseDragged(double mouseX, double mouseY, int button, double dragX, double dragY) {
		for (GuiEventListener child : children()) {
			if (child.isMouseOver(mouseX, mouseY) && child.mouseDragged(mouseX, mouseY, button, dragX, dragY)) {
				return true;
			}
		}
		Slot slot = findSlot(mouseX, mouseY);
		ItemStack itemstack = getMenu().getCarried();
		if (isQuickCrafting && slot != null && !itemstack.isEmpty()
				&& (itemstack.getCount() > quickCraftSlots.size() || quickCraftingType == 2)
				&& StorageContainerMenuBase.canItemQuickReplace(slot, itemstack) && slot.mayPlace(itemstack)
				&& menu.canDragTo(slot)) {
			quickCraftSlots.add(slot);
			recalculateQuickCraftRemaining();
		}

		return super.mouseDragged(mouseX, mouseY, button, dragX, dragY);
	}

	@Override
	protected boolean hasClickedOutside(double mouseX, double mouseY, int guiLeftIn, int guiTopIn, int mouseButton) {
		return super.hasClickedOutside(mouseX, mouseY, guiLeftIn, guiTopIn, mouseButton) && hasClickedOutsideOfUpgradeSlots(mouseX, mouseY)
				&& hasClickedOutsideOfUpgradeSettings(mouseX, mouseY);
	}

	private boolean hasClickedOutsideOfUpgradeSettings(double mouseX, double mouseY) {
		return settingsTabControl.getTabRectangles().stream().noneMatch(r -> r.contains((int) mouseX, (int) mouseY));
	}

	private boolean hasClickedOutsideOfUpgradeSlots(double mouseX, double mouseY) {
		return !getUpgradeSlotsRectangle().map(r -> r.contains((int) mouseX, (int) mouseY)).orElse(false);
	}

	public Optional<Rect2i> getUpgradeSlotsRectangle() {
		return numberOfUpgradeSlots == 0 ? Optional.empty() : Optional.of(new Rect2i(leftPos - UPGRADE_INVENTORY_OFFSET, topPos + getUpgradeTop(), 32, getUpgradeHeight()));
	}

	private void renderStackCount(String count, int x, int y) {
		PoseStack posestack = new PoseStack();
		posestack.translate(0.0D, 0.0D, itemRenderer.blitOffset + 200.0F);
		float scale = Math.min(1f, (float) 16 / font.width(count));
		if (scale < 1f) {
			posestack.scale(scale, scale, 1.0F);
		}
		MultiBufferSource.BufferSource renderBuffer = MultiBufferSource.immediate(Tesselator.getInstance().getBuilder());
		font.drawInBatch(count, (x + 19 - 2 - (font.width(count) * scale)) / scale,
				(y + 6 + 3 + (1 / (scale * scale) - 1)) / scale, 16777215, true, posestack.last().pose(), renderBuffer, false, 0, 15728880);
		renderBuffer.endBatch();
	}

	@Override
	protected void recalculateQuickCraftRemaining() {
		ItemStack cursorStack = getMenu().getCarried();
		if (!cursorStack.isEmpty() && isQuickCrafting) {
			if (quickCraftingType == 2) {
				quickCraftingRemainder = cursorStack.getMaxStackSize();
			} else {
				quickCraftingRemainder = cursorStack.getCount();

				for (Slot slot : quickCraftSlots) {
					ItemStack itemstack1 = cursorStack.copy();
					ItemStack slotStack = slot.getItem();
					int slotStackCount = slotStack.isEmpty() ? 0 : slotStack.getCount();
					AbstractContainerMenu.getQuickCraftSlotCount(quickCraftSlots, quickCraftingType, itemstack1, slotStackCount);
					int j = slot.getMaxStackSize(itemstack1);
					if (itemstack1.getCount() > j) {
						itemstack1.setCount(j);
					}

					quickCraftingRemainder -= itemstack1.getCount() - slotStackCount;
				}
			}
		}
	}

	private void renderErrorOverlay(PoseStack matrixStack) {
		menu.getErrorUpgradeSlotChangeResult().ifPresent(upgradeSlotChangeResult -> upgradeSlotChangeResult.getErrorMessage().ifPresent(overlayErrorMessage -> {
			matrixStack.pushPose();
			matrixStack.translate(getGuiLeft(), getGuiTop(), 0.0F);
			upgradeSlotChangeResult.getErrorUpgradeSlots().forEach(slotIndex -> renderSlotOverlay(matrixStack, menu.getSlot(menu.getFirstUpgradeSlot() + slotIndex), ERROR_SLOT_COLOR));
			upgradeSlotChangeResult.getErrorInventorySlots().forEach(slotIndex -> {
				Slot slot = menu.getSlot(slotIndex);
				if (slot != null) {
					renderSlotOverlay(matrixStack, slot, ERROR_SLOT_COLOR);
				}
			});
			upgradeSlotChangeResult.getErrorInventoryParts().forEach(partIndex -> {
				if (inventoryParts.size() > partIndex) {
					inventoryParts.get(partIndex).renderErrorOverlay(matrixStack);
				}
			});
			matrixStack.popPose();

			renderErrorMessage(matrixStack, overlayErrorMessage);
		}));
	}

	private void renderErrorMessage(PoseStack matrixStack, Component overlayErrorMessage) {
		matrixStack.pushPose();
		RenderSystem.disableDepthTest();
		matrixStack.translate((float) width / 2, (double) topPos + inventoryLabelY + 4, 300F);
		Font fontrenderer = Minecraft.getInstance().font;

		int tooltipWidth = font.width(overlayErrorMessage);

		List<FormattedText> wrappedTextLines = new ArrayList<>();
		int maxLineWidth = 260;
		if (tooltipWidth > maxLineWidth) {
			int wrappedTooltipWidth = 0;
			List<FormattedText> wrappedLine = font.getSplitter().splitLines(overlayErrorMessage, maxLineWidth, Style.EMPTY);

			for (FormattedText line : wrappedLine) {
				int lineWidth = font.width(line);
				if (lineWidth > wrappedTooltipWidth) {wrappedTooltipWidth = lineWidth;}
				wrappedTextLines.add(line);
			}
			tooltipWidth = wrappedTooltipWidth;
		} else {
			wrappedTextLines.add(overlayErrorMessage);
		}

		int tooltipHeight = 8;
		if (wrappedTextLines.size() > 1) {
			tooltipHeight += 2 + (wrappedTextLines.size() - 1) * 10;
		}

		Matrix4f matrix4f = matrixStack.last().pose();
		float leftX = (float) -tooltipWidth / 2;

		GuiHelper.renderTooltipBackground(matrix4f, tooltipWidth, (int) leftX, 0, tooltipHeight, StorageScreenBase.ERROR_BACKGROUND_COLOR, StorageScreenBase.ERROR_BORDER_COLOR, StorageScreenBase.ERROR_BORDER_COLOR);
		MultiBufferSource.BufferSource renderTypeBuffer = MultiBufferSource.immediate(Tesselator.getInstance().getBuilder());
		matrixStack.translate(0.0D, 0.0D, 400.0D);
		GuiHelper.writeTooltipLines(wrappedTextLines, fontrenderer, leftX, 0, matrix4f, renderTypeBuffer, ERROR_TEXT_COLOR);
		renderTypeBuffer.endBatch();
		matrixStack.popPose();
	}
}

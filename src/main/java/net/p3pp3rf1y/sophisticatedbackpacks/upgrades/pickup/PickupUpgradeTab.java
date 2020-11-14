package net.p3pp3rf1y.sophisticatedbackpacks.upgrades.pickup;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.inventory.container.Slot;
import net.minecraft.util.ResourceLocation;
import net.minecraft.util.text.ITextComponent;
import net.minecraft.util.text.TranslationTextComponent;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.BackpackScreen;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UpgradeSettingsTab;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BooleanToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;

import java.util.function.Consumer;
import java.util.function.Predicate;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.*;

public abstract class PickupUpgradeTab extends UpgradeSettingsTab<PickupUpgradeContainer> {
	protected final int slotsLeftX;
	protected int slotsTopY;
	private final Consumer<PickupUpgradeTab> onOpen;
	private final Consumer<PickupUpgradeTab> onClose;
	private final BackpackScreen screen;
	private static final String BACKPACK_54 = "textures/gui/backpack_54.png";
	private static final TextureBlitData SLOT_BACKGROUND_9_SLOTS = new TextureBlitData(new ResourceLocation(SophisticatedBackpacks.MOD_ID, BACKPACK_54), new UV(7, 17), new Dimension(54, 54));
	private static final TextureBlitData SLOT_BACKGROUND_16_SLOTS = new TextureBlitData(new ResourceLocation(SophisticatedBackpacks.MOD_ID, BACKPACK_54), new UV(7, 17), new Dimension(72, 72));

	public PickupUpgradeTab(PickupUpgradeContainer upgradeContainer, Position position, Dimension openTabDimension, BackpackScreen screen, Consumer<PickupUpgradeTab> onOpen, Consumer<PickupUpgradeTab> onClose) {
		super(upgradeContainer, position, openTabDimension);
		this.screen = screen;
		this.onOpen = onOpen;
		this.onClose = onClose;
		addHideableChild(getButton(new Position(x + 3, y + 24), button -> {
			getContainer().setAllowList(!getContainer().isAllowList());
			return true;
		}, new UV(32, 32), new UV(48, 32), () -> getContainer().isAllowList(), translUpgradeButton("allow"), translUpgradeButton("block")));
		slotsLeftX = x + 4;
		slotsTopY = y + 46;
	}

	protected ToggleButton<Boolean> getButton(Position pos, Predicate<Integer> onClick, UV onUV, UV offUV, Supplier<Boolean> getState, String onTooltip, String offTooltip) {
		ToggleButton<Boolean> blockAllowButton = new BooleanToggleButton(pos, Dimension.SQUARE_18, onClick, new TextureBlitData(UPGRADE_CONTROLS, new UV(29, 0), Dimension.SQUARE_18),
				getButtonStateData(onUV, onTooltip),
				getButtonStateData(offUV, offTooltip),
				getState);
		blockAllowButton.setHoveredBackgroundTexture(new TextureBlitData(UPGRADE_CONTROLS, new UV(47, 0), new Dimension(18, 18)));
		return blockAllowButton;
	}

	private static ToggleButton.StateData getButtonStateData(UV uv, String tooltip) {
		return new ToggleButton.StateData(new TextureBlitData(UPGRADE_CONTROLS, new Position(1, 1), Dimension.SQUARE_256, uv, Dimension.SQUARE_16),
				new TranslationTextComponent(tooltip)
		);
	}

	@Override
	protected void onTabOpen() {
		super.onTabOpen();
		moveSlotsToTab();

		onOpen.accept(this);
	}

	private void moveSlotsToTab() {
		int upgradeSlotNumber = 0;
		for (Slot slot : getContainer().getSlots()) {
			slot.xPos = slotsLeftX - screen.getGuiLeft() + (upgradeSlotNumber % getSlotsPerRow()) * 18;
			slot.yPos = slotsTopY - screen.getGuiTop() + (upgradeSlotNumber / getSlotsPerRow()) * 18;
			upgradeSlotNumber++;
		}
	}

	protected abstract int getSlotsPerRow();

	private void moveSlotsOutOfView() {
		getContainer().getSlots().forEach(slot -> {
			slot.xPos = -100;
			slot.yPos = -100;
		});

	}

	@Override
	protected void onTabClose() {
		super.onTabClose();

		moveSlotsOutOfView();
		onClose.accept(this);
	}

	public static class Basic extends PickupUpgradeTab {
		public Basic(PickupUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, Consumer<PickupUpgradeTab> onOpen, Consumer<PickupUpgradeTab> onClose) {
			super(upgradeContainer, position, new Dimension(63, 106), screen, onOpen, onClose);
		}

		@Override
		protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
			super.renderBg(matrixStack, minecraft, mouseX, mouseY);
			if (isOpen()) {
				GuiHelper.blit(minecraft, matrixStack, slotsLeftX - 1, slotsTopY - 1, SLOT_BACKGROUND_9_SLOTS);
			}
		}

		@Override
		protected int getSlotsPerRow() {
			return 3;
		}

		@Override
		protected ITextComponent getTabLabel() {
			return new TranslationTextComponent(translUpgrade("pickup"));
		}

		@Override
		protected ITextComponent getClosedTooltip() {
			return new TranslationTextComponent(translUpgradeTooltip("pickup"));
		}
	}

	public static class Advanced extends PickupUpgradeTab {
		public Advanced(PickupUpgradeContainer upgradeContainer, Position position, BackpackScreen screen, Consumer<PickupUpgradeTab> onOpen, Consumer<PickupUpgradeTab> onClose) {
			super(upgradeContainer, position, new Dimension(81, 124), screen, onOpen, onClose);

			addHideableChild(getPrimaryMatchButton());
			addHideableChild(getDurabilityButton());
			addHideableChild(getNbtButton());
		}

		@Override
		protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
			super.renderBg(matrixStack, minecraft, mouseX, mouseY);
			if (isOpen()) {
				GuiHelper.blit(minecraft, matrixStack, slotsLeftX - 1, slotsTopY - 1, SLOT_BACKGROUND_16_SLOTS);
			}
		}

		@Override
		protected int getSlotsPerRow() {
			return 4;
		}

		@Override
		protected ITextComponent getTabLabel() {
			return new TranslationTextComponent(translUpgrade("advanced_pickup"));
		}

		@Override
		protected ITextComponent getClosedTooltip() {
			return new TranslationTextComponent(translUpgradeTooltip("advanced_pickup"));
		}

		protected ToggleButton<Boolean> getDurabilityButton() {
			return getButton(new Position(x + 39, y + 24), button -> {
						getContainer().setMatchDurability(!getContainer().shouldMatchDurability());
						return true;
					}, new UV(32, 48), new UV(48, 48), () -> getContainer().shouldMatchDurability(),
					translUpgradeButton("match_durability"), translUpgradeButton("ignore_durability"));
		}

		protected ToggleButton<Boolean> getNbtButton() {
			return getButton(new Position(x + 57, y + 24), button -> {
						getContainer().setMatchNbt(!getContainer().shouldMatchNbt());
						return true;
					}, new UV(64, 32), new UV(80, 32), () -> getContainer().shouldMatchNbt(),
					translUpgradeButton("match_nbt"), translUpgradeButton("ignore_nbt"));
		}

		private ToggleButton<PrimaryMatch> getPrimaryMatchButton() {
			ToggleButton<PrimaryMatch> blockAllowButton = new ToggleButton<>(new Position(x + 21, y + 24), Dimension.SQUARE_18, button -> {
				getContainer().setPrimaryMatch(getContainer().getPrimaryMatch().next());
				return true;
			}, new TextureBlitData(UPGRADE_CONTROLS, new UV(29, 0), Dimension.SQUARE_18),
					ImmutableMap.of(
							PrimaryMatch.ITEM, getButtonStateData(new UV(80, 48), translUpgradeButton("match_item")),
							PrimaryMatch.MOD, getButtonStateData(new UV(64, 48), translUpgradeButton("match_mod")),
							PrimaryMatch.TAGS, getButtonStateData(new UV(96, 32), translUpgradeButton("match_tags"))
					),
					() -> getContainer().getPrimaryMatch());
			blockAllowButton.setHoveredBackgroundTexture(new TextureBlitData(UPGRADE_CONTROLS, new UV(47, 0), new Dimension(18, 18)));
			return blockAllowButton;
		}
	}
}

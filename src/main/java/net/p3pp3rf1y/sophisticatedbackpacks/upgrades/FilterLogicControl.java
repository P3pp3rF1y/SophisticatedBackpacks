package net.p3pp3rf1y.sophisticatedbackpacks.upgrades;

import com.google.common.collect.ImmutableMap;
import com.mojang.blaze3d.matrix.MatrixStack;
import net.minecraft.client.Minecraft;
import net.minecraft.util.ResourceLocation;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.p3pp3rf1y.sophisticatedbackpacks.SophisticatedBackpacks;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Dimension;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.Position;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TextureBlitData;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.UV;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.BooleanToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.CompositeWidget;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.ToggleButton;
import net.p3pp3rf1y.sophisticatedbackpacks.client.gui.controls.Widget;

import java.util.function.Predicate;
import java.util.function.Supplier;

import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.GuiHelper.UPGRADE_CONTROLS;
import static net.p3pp3rf1y.sophisticatedbackpacks.client.gui.TranslationHelper.translUpgradeButton;

@OnlyIn(Dist.CLIENT)
public class FilterLogicControl extends CompositeWidget<Widget> {
	private static final String BACKPACK_54 = "textures/gui/backpack_54.png";
	private static final TextureBlitData SLOT_BACKGROUND_9_SLOTS = new TextureBlitData(new ResourceLocation(SophisticatedBackpacks.MOD_ID, BACKPACK_54), new UV(7, 17), new Dimension(54, 54));
	private static final TextureBlitData SLOT_BACKGROUND_16_SLOTS = new TextureBlitData(new ResourceLocation(SophisticatedBackpacks.MOD_ID, BACKPACK_54), new UV(7, 17), new Dimension(72, 72));

	protected final FilterLogicContainer container;

	public FilterLogicControl(Position position, IFilteredUpgradeContainer filteredContainer) {
		super(position);
		container = filteredContainer.getFilterLogicContainer();
		addChild(getButton(new Position(x, y), button -> {
			container.setAllowList(!container.isAllowList());
			return true;
		}, new UV(32, 32), new UV(48, 32), container::isAllowList, translUpgradeButton("allow"), translUpgradeButton("block")));
	}

	public FilterLogicControl showPrimaryMatchButton() {
		addChild(getPrimaryMatchButton());
		return this;
	}

	protected ToggleButton<Boolean> getButton(Position pos, Predicate<Integer> onClick, UV onUV, UV offUV, Supplier<Boolean> getState, String onTooltip, String offTooltip) {
		ToggleButton<Boolean> blockAllowButton = new BooleanToggleButton(pos, Dimension.SQUARE_18, onClick, new TextureBlitData(UPGRADE_CONTROLS, new UV(29, 0), Dimension.SQUARE_18),
				GuiHelper.getButtonStateData(onUV, onTooltip),
				GuiHelper.getButtonStateData(offUV, offTooltip),
				getState);
		blockAllowButton.setHoveredBackgroundTexture(new TextureBlitData(UPGRADE_CONTROLS, new UV(47, 0), new Dimension(18, 18)));
		return blockAllowButton;
	}

	private ToggleButton<PrimaryMatch> getPrimaryMatchButton() {
		ToggleButton<PrimaryMatch> blockAllowButton = new ToggleButton<>(new Position(x + 18, y), Dimension.SQUARE_18, button -> {
			container.setPrimaryMatch(container.getPrimaryMatch().next());
			return true;
		}, GuiHelper.DEFAULT_BUTTON_BACKGROUND,
				ImmutableMap.of(
						PrimaryMatch.ITEM, GuiHelper.getButtonStateData(new UV(80, 48), translUpgradeButton("match_item")),
						PrimaryMatch.MOD, GuiHelper.getButtonStateData(new UV(64, 48), translUpgradeButton("match_mod")),
						PrimaryMatch.TAGS, GuiHelper.getButtonStateData(new UV(96, 32), translUpgradeButton("match_tags"))
				),
				container::getPrimaryMatch);
		blockAllowButton.setHoveredBackgroundTexture(GuiHelper.DEFAULT_BUTTON_HOVERED_BACKGROUND);
		return blockAllowButton;
	}

	@Override
	protected void renderBg(MatrixStack matrixStack, Minecraft minecraft, int mouseX, int mouseY) {
		GuiHelper.blit(minecraft, matrixStack, x, y + 21, container.getFilterSlots() == 9 ? SLOT_BACKGROUND_9_SLOTS : SLOT_BACKGROUND_16_SLOTS);
	}

	@Override
	public int getWidth() {
		return 54;
	}

	@Override
	public int getHeight() {
		return 75;
	}

	public static class Advanced extends FilterLogicControl {

		public Advanced(Position position, IFilteredUpgradeContainer container) {
			super(position, container);

			showPrimaryMatchButton();
			addChild(getDurabilityButton());
			addChild(getNbtButton());
		}

		private ToggleButton<Boolean> getDurabilityButton() {
			return getButton(new Position(x + 36, y), button -> {
						container.setMatchDurability(!container.shouldMatchDurability());
						return true;
					}, new UV(32, 48), new UV(48, 48), container::shouldMatchDurability,
					translUpgradeButton("match_durability"), translUpgradeButton("ignore_durability"));
		}

		private ToggleButton<Boolean> getNbtButton() {
			return getButton(new Position(x + 54, y), button -> {
						container.setMatchNbt(!container.shouldMatchNbt());
						return true;
					}, new UV(64, 32), new UV(80, 32), container::shouldMatchNbt,
					translUpgradeButton("match_nbt"), translUpgradeButton("ignore_nbt"));
		}

		@Override
		public int getWidth() {
			return 72;
		}

		@Override
		public int getHeight() {
			return 93;
		}
	}
}
